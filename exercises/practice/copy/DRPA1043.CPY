      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1043.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1043.CPv  $
      *
      *   Rev 2.0   May 27 1999 13:24:48   T01JM2S
      *APA00042
      *
      *   Rev 1.0   14 Jul 1998 14:00:38   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1043                                      *
      *    --------------------------------------------------------   *
      *    EVS-NOTIZ                                                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    12.08.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!12.08.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN ZUR NOTIZ "KD-BETREUER-     *
      *    STRUKTUR ANPASSEN"                                        *
      *                                                              *
      ****************************************************************
      *
       01  DRPA1043-NZPA0009.
           05 DRPA1043-ZEILE-1.
              10 FILLER                             PIC  X(025)
                                     VALUE 'NZPA0009 Kundenbetreuer: '.
      *
              10 DRPA1043-KD-BETR                   PIC  9(007).
              10 FILLER                             PIC  X(014)
                                     VALUE '  Fällig von: '.
              10 DRPA1043-FAELLIG-VON               PIC  X(010).
              10 FILLER                             PIC  X(009)
                                     VALUE '  VSTAT: '.
              10 DRPA1043-VSTAT                     PIC  X(004).
              10 DRPA1043-FILLER-ZL-1               PIC  X(011).
      *
      *
       01  DRPA1043-NOTIZZEILEN REDEFINES DRPA1043-NZPA0009.
           05 DRPA1043-NOTIZZEILE                   OCCURS 1
                                             INDEXED BY DRPA1043-NZ-IX.
              10 FILLER                             PIC  X(080).
