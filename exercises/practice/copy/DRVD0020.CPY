      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.12.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0020.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVD0020.CPv  $
      *
      *   Rev 2.2   30 Oct 2007 08:58:16   t13ae23
      *AAN00765: VM-Anteil im EGG für VBI bereitstellen
      *
      *   Rev 2.1   Feb 01 2005 10:33:28   t13ae23
      *AVD00007: Bereitstellung eingereichtes Geschäft.
      *
      *   Rev 2.0   Dec 20 2004 15:14:44   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *  COPY-MEMBER UMSETZEN EINGEREICHTES GESCHÄFT                 *
      *              IN FORMAT FÜR VBI                               *
      *                                                              *
      ****************************************************************
      * LOGBUCH                                                      *
      * AUFTRAG ! DATUM  ! AENDERUNG                 ! NAME          *
      * --------+--------+---------------------------+---------------*
      * AVD00007!17.01.05! NEU                       ! RAHNER        *
      * AAN00765!30.10.07! VM-KENNZEICHEN            ! RAHNER        *
      *         !        !                           !               *
      ****************************************************************
       01  DRVD0020-VBI-SATZ.
           05 DRVD0020-MA-ST-NR              PIC S9(09)    COMP.
           05 DRVD0020-VBI-STAT-DAT-VON      PIC  X(10).
           05 DRVD0020-KNR-PROD-GB-GES       PIC S9(04)    COMP.
           05 DRVD0020-PRPS-PROD-SCHL        PIC S9(04)    COMP.
           05 DRVD0020-KENZ-PROD-SCHL-DIVE   PIC  X(01).
301007     05 DRVD0020-KENZ-PROD-SCHL-VM     PIC  X(01).
           05 DRVD0020-MS-NR                 PIC S9(04)    COMP.
           05 DRVD0020-VBI-BETR-NR           PIC S9(09)    COMP.
           05 DRVD0020-VBI-TAGES-DAT         PIC  X(10).
           05 DRVD0020-KTY-PROD-GB-GES       PIC S9(04)    COMP.
           05 DRVD0020-KNR-ERGEBNIS-TYP      PIC S9(04)    COMP.
           05 DRVD0020-KTY-ERGEBNIS-TYP      PIC S9(04)    COMP.
           05 DRVD0020-VBI-EH-NT-EG          PIC S9(10)V9  COMP-3.
           05 DRVD0020-VBI-ANZ-AN-EG         PIC S9(04)    COMP.
           05 DRVD0020-VBI-VS-SUM-EG         PIC S9(11)    COMP-3.
           05 DRVD0020-VBI-BEITRAG-EG        PIC S9(09)V99 COMP-3.
           05 DRVD0020-KNR-WAEHRUNG          PIC S9(04)    COMP.
           05 DRVD0020-KTY-WAEHRUNG          PIC S9(04)    COMP.
           05 DRVD0020-VBI-EH-NT-GR          PIC S9(10)V9  COMP-3.
           05 DRVD0020-VBI-ANZ-AN-GR         PIC S9(04)    COMP.
           05 DRVD0020-VBI-VS-SUM-GR         PIC S9(11)    COMP-3.
           05 DRVD0020-VBI-BEITRAG-GR        PIC S9(09)V99 COMP-3.
           05 DRVD0020-VBI-FILLER            PIC X(79).
