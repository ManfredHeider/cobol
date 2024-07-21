      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1042.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1042.CPv  $
      *
      *   Rev 2.0   May 27 1999 13:23:34   T01JM2S
      *APA00042
      *
      *   Rev 1.0   14 Jul 1998 14:00:34   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1042                                      *
      *    --------------------------------------------------------   *
      *    EVS-NOTIZ                                                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    11.08.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.08.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN ZUR NOTIZ "AVERM PRÜFEN".   *
      *                                                              *
      ****************************************************************
      *
       01  DRPA1042-NZPA0007.
           05 DRPA1042-ZEILE-1.
              10 FILLER                             PIC  X(016)
                                           VALUE 'NZPA0007 AVERM: '.
      *
              10 DRPA1042-AVERM                     PIC  9(007).
              10 FILLER                             PIC  X(014)
                                           VALUE '  Fällig von: '.
              10 DRPA1042-FAELLIG-VON               PIC  X(010).
              10 FILLER                             PIC  X(010)
                                           VALUE '  VSTAT : '.
              10 DRPA1042-VSTAT                     PIC  X(004).
              10 DRPA1042-FILLER-ZL-1               PIC  X(019).
      *
      *
       01  DRPA1042-NOTIZZEILEN REDEFINES DRPA1042-NZPA0007.
           05 DRPA1042-NOTIZZEILE                   OCCURS 1
                                             INDEXED BY DRPA1042-NZ-IX.
              10 FILLER                             PIC  X(080).
