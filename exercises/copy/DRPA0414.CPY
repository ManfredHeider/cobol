      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0414.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0414.cpv  $
      *
      *   Rev 2.0   May 27 1999 13:31:28   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:33:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0414                                      *
      *    --------------------------------------------------------   *
      *    PEBD-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    22.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.10.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * apa00012!18.08.98! 02 ! PEST-ID und Positionsnumm.! Parent   *
      *         !        !    ! entfernt ( Denormalisier) !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR PEBD.                   *
      *                                                              *
      ****************************************************************
      *
             30 PEBD-DATEN.
180898*         35 PEST-ID                          PIC S9(015)
180898*                                                 PACKED-DECIMAL.
180898*         35 PEBD-POSITIONS-NR                PIC S9(004) BINARY.
                35 PEBD-ST-NR                       PIC S9(009) BINARY.
                35 PEBD-EMP-STF                     PIC  X(004).
                35 PEBD-DIFF-PROV-STF               PIC S9(004) BINARY.
                35 KNR-EMP-TYP                      PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
