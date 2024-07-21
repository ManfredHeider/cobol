      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1150.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1150.CPv  $
      *
      *   Rev 2.1   May 25 1999 14:15:20   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 25 1999 14:10:16   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:00:56   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1150 EVS - AUSGANGSDATEN                 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                       *
      *    DATUM:    22.12.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!22.12.98! 01 !NEU                        ! Bohlinger*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1150                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************

            15 EVS-DATEN.
               20 PMBE-ID                      PIC S9(015)
                                               PACKED-DECIMAL.
               20 PMBE-VERSION                 PIC S9(004) BINARY.
               20 FAELLIG-VON                  PIC  X(010).
               20 FAELLIG-BIS                  PIC  X(010).
               20 VM                           PIC S9(009) BINARY.
               20 AVERM                        PIC S9(009) BINARY.
               20 AVERM-PRZ                    PIC  X(001).
               20 PEST-ID                      PIC S9(015)
                                               PACKED-DECIMAL.

            15 VORVERTRAGSDATEN.
               20 MAX-USED-VVNR                PIC S9(004) BINARY.
               20 VORVERTRAGS-TAB              OCCURS 9.
                  25 VVNR                      PIC  X(015).
                  25 KDBETR                    PIC S9(009) BINARY.

            15 PRODUKTDATEN.
               20 MAX-USED-PROD                PIC S9(004) BINARY.
               20 PRODUKT-TAB                  OCCURS 9.

                 25 PVPT-ID                    PIC S9(015)
                                               PACKED-DECIMAL.
                 25 PVPT-VERSION               PIC S9(004) BINARY.
                 25 PROD-PROD-SCHL             PIC S9(004) BINARY.
                 25 PVPT-LAUFZEIT              PIC S9(004) BINARY.
                 25 MIND-LAUFZEIT              PIC S9(004) BINARY.
                 25 JAHRES-NT-PRAEMIE-EVS      PIC S9(009)V9(2)
                                               PACKED-DECIMAL.
                 25 JAHRES-NT-PRAEMIE-VVNR     PIC S9(009)V9(2)
                                               PACKED-DECIMAL.
                 25 JNP-ERHOEHUNG-PROZ         PIC S9(004)V9(2).
                 25 EE-ENTSCHEIDUNG            PIC X(002).
