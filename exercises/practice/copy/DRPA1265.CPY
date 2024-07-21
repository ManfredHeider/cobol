      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1265.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1265.CPv  $
      *
      *   Rev 2.1   Aug 23 1999 09:57:28   T13EX44
      *Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:18   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:48   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1265                                      *
      *    --------------------------------------------------------   *
      *    RESTARTINFORMATIONEN ZU PMPA0043                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                           *
      *    DATUM:    15.06.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!15.06.99!2.0 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHŽLT DIE DATEN MIT DEN RESTARTINFORMA-     *
      *    TIONEN ZU PMPA0043 (SST-ZUKŠNFTIGE FŽLLIGKEITEN)          *
      *                                                              *
      ****************************************************************
      *
       01  DRPA1265-RESTART.
           05 DRPA1265-ID.
              10 FILLER                             PIC  X(008)
                                                      VALUE 'DRPA1265'.
      *
           05 DRPA1265-DATEN.
      *    KEY-FELD EINGABEDATEI FEHLDAT
              10 DRPA1265-KEY.
                 15 DRPA1265-GES                PIC 99.
                 15 DRPA1265-VSNR               PIC 9(15).
                 15 DRPA1265-JH                 PIC 99.
                 15 DRPA1265-FAELLIG            PIC 9(6).

      *    GRUPPENWECHSELFELDER
              10 DRPA1265-NEU                   PIC X(26).
              10 DRPA1265-ALT                   PIC X(26).

      *    SCHALTER
              10  DRPA1265-AELTERE-FAELLIG-VORH PIC  9(001).
              10  DRPA1265-ABH-SEG-GELESEN      PIC  9(001).
              10  DRPA1265-ABH-SEG-VORH         PIC  9(001).
      *        K-VERTRAG ? (FEHLERDATEI/VSNR-> '8' AN 4.STELLE)
              10  DRPA1265-K-VERTRAG            PIC  9(001).
      *
      *    Z-FELDER
              05  DRPA1265-ANZ-EFILE-READ       PIC S9(009).
              05  DRPA1265-ANZAHL-FEHLDAT       PIC S9(009).
              05  DRPA1265-ANZAHL-VERARBEITET   PIC S9(009).
              05  DRPA1265-ANZAHL-OHNE-BSTDTA   PIC S9(009).
              05  DRPA1265-ANZAHL-AUSDTA        PIC S9(009).
              05  DRPA1265-ANZAHL-AUSDTA-RESTART
                                                PIC S9(009).
              05  DRPA1265-SUMMEGUTSCHRIFT    PIC S9(009)V9(002) COMP-3.
              05  DRPA1265-SUMMEBELASTUNG     PIC S9(009)V9(002) COMP-3.

              05  DRPA1265-BST-DTA-SATZ          PIC  X(161).
      **
