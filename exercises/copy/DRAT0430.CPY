      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0430.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0430.CPv  $
      *
      *   Rev 2.3   Apr 18 2016 15:04:30   t13ex71
      *ARW00109: Add Prüfung KNR=13 + Textanpassung "VBA..." in PMR
      *W0017 (BPR)
      *
      *   Rev 2.2   Aug 13 2008 12:37:30   t13ae35
      *AVB00990: Übernahme Badenia-Aussendienst
      *
      *   Rev 2.1   05 Oct 2007 14:03:04   t13ae35
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung ->
      *PMRW0017
      *
      *   Rev 2.0   04 Sep 2007 15:32:24   t13ae35
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0319  KONSTANTEN FÜR KTY 85              *
      *                           SWNP-Berechnungsstati              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J.Hering                                        *
      *    DATUM:    27.08.2007                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARW00041!27.08.07!2.1 ! NEU                       ! J.Hering *
      * AVB00990!13.08.08!2.2 ! Badenia in 2008 kein SWNP ! J.Hering *
      * ARW00109!13.04.16!2.3 ! Lizenzgebühr nur wenn     ! B.Pretsch*
      *         !        !    ! Premium-Lizenz akzeptiert !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
           05 K-KNR-BERECHNET             PIC S9(004) BINARY VALUE  1.
           05 K-KNR-NB-ALP                PIC S9(004) BINARY VALUE  2.
           05 K-KNR-NB-VBA                PIC S9(004) BINARY VALUE  3.
           05 K-KNR-NB-VM                 PIC S9(004) BINARY VALUE  4.
           05 K-KNR-NB-AUSTRITT           PIC S9(004) BINARY VALUE  5.
           05 K-KNR-NB-BESCHR             PIC S9(004) BINARY VALUE  6.
           05 K-KNR-NB-KEIN-VERDIENST     PIC S9(004) BINARY VALUE  7.
           05 K-KNR-NB-KEIN-MA            PIC S9(004) BINARY VALUE  8.
           05 K-KNR-NB-STUFE-UNBEK        PIC S9(004) BINARY VALUE  9.
           05 K-KNR-NB-MINIMAL-UNT        PIC S9(004) BINARY VALUE 10.
           05 K-KNR-NB-KEIN-AD            PIC S9(004) BINARY VALUE 11.
           05 K-KNR-NB-BADENIA            PIC S9(004) BINARY VALUE 12.
           05 K-KNR-NB-KEINE-PREM-LIZ-AKZ PIC S9(004) BINARY VALUE 13.
 

