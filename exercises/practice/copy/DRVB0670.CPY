      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0670.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0670.CPv  $
      *
      *   Rev 2.2   Jul 08 2005 11:08:24   t13ex13
      *AAS00560: 
      *Auswertung Leistungsparameter der Direktionen
      *
      *   Rev 2.1   Sep 17 2004 11:15:36   t13ex22
      *AAS00425: Vergleich Beförderungsbed. RGS - RD2
      *
      *   Rev 2.0   Feb 17 2003 10:59:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: SATZBESCHREIBUNG AUSGABEDATEI FUER PMAS0044   *
      *                                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: UTE BOLL                                         *
      *****************************************************************
      *    VERSION   01   25.02.1994                                  *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAS00425!19.08.03! 01 ! NEU                       ! U.BOLL    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      ****************************************************************
      *
      *   AUSGABE-DATEI
      *
        01  DRVB0670.
           05 DRVB0670-MS-NR             PIC  9(04).
           05 FILLER                     PIC  X(01).
           05 DRVB0670-STUFE-BEZ         PIC  X(04).
           05 FILLER                     PIC  X(01).
           05 DRVB0670-VBNR              PIC  9(07).
           05 FILLER                     PIC  X(01).
           05 DRVB0670-DIR               PIC  Z(03)9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-REGION            PIC  X(10).
           05 FILLER                     PIC  X(01).
           05 DRVB0670-DL-NAME           PIC  X(40).
           05 FILLER                     PIC  X(01).
           05 DRVB0670-NAME              PIC  X(40).
           05 FILLER                     PIC  X(01).
           05 DRVB0670-VORNAME           PIC  X(20).
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EH-EIG-ALT        PIC -9(09),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EH-EIG-KZ-ALT     PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EQF-EIG-ALT       PIC -9(03),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EQF-EIG-KZ-ALT    PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EH-GRU-ALT        PIC -9(09),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EH-GRU-KZ-ALT     PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EQF-GRU-ALT       PIC -9(03),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EQF-GRU-KZ-ALT    PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EH-GES-ALT        PIC -9(09),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EH-GES-KZ-ALT     PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EQF-GES-ALT       PIC -9(03),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EQF-GES-KZ-ALT    PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-PROZ-KLAUS-ALT    PIC -9(09),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EIGEN-MAX-ALT     PIC -9(09),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-KORR-EH-GRU-ALT   PIC -9(09),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-KORR-EH-GES-ALT   PIC -9(09),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-KORR-KZ-ALT       PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-ALT-KZ            PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EH-EIG-NEU        PIC -9(09),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EH-EIG-KZ-NEU     PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EQF-EIG-NEU       PIC -9(03),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EQF-EIG-KZ-NEU    PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EH-GRU-NEU        PIC -9(09),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EH-GRU-KZ-NEU     PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EQF-GRU-NEU       PIC -9(03),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EQF-GRU-KZ-NEU    PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EH-GES-NEU        PIC -9(09),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EH-GES-KZ-NEU     PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EQF-GES-NEU       PIC -9(03),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EQF-GES-KZ-NEU    PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-PROZ-KLAUS-NEU    PIC -9(09),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-EIGEN-MAX-NEU     PIC -9(09),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-KORR-EH-GES-NEU   PIC -9(09),9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-KORR-EH-GES-KZ-NEU  PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-PART-NEU          PIC  Z(02)9.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-AP-KZ-NEU         PIC  X.
           05 FILLER                     PIC  X(01).
           05 DRVB0670-NEU-KZ            PIC  X.
           05 FILLER                     PIC  X(615).
      *
      ******** ENDE COPY-MEMBER (DRVB0670) ************
      *
