      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0484.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0484.cpv  $
      *
      *   Rev 2.0   May 26 1999 16:59:10   T13EX25
      *APA00042, Erfassung V2.0
      *
      *
      *   Rev 1.0   23 Jul 1997 12:42:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0484  Tabellenelement PEBD               *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                   *
      *    DATUM:    26.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !26.08.97! 01 ! NEUERSTELLUNG             ! Jäger    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * apa00012!18.08.98! 02 ! PEST-ID und Positionsnumm.! Parent   *
      *         !        !    ! entfernt ( Denormalisier) !          *
      *                                                              *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
180898*          35 PEBD-KEY.
180898*             40 PEBD-PEST-ID          PIC S9(015) PACKED-DECIMAL.
180898*             40 POSITIONS-NR          PIC S9(004) BINARY.
                 35 ST-NR                    PIC S9(009) BINARY.
                 35 PRFZ                     PIC  X(001).
                 35 EMP-STF                  PIC  X(004).
                 35 DIFF-PROV-STF
                                             PIC S9(004) BINARY.
                 35 KTY-EMP-TYP              PIC S9(004) BINARY.
                 35 KNR-EMP-TYP              PIC S9(004) BINARY.
