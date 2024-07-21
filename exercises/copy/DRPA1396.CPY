      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1396.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1396.CPv  $
      *
      *   Rev 2.3   16 May 2007 06:45:52   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.2   07 Jul 2000 11:10:28   T13EX23
      *FPA00166 - RC-NOK jetzt RC-NOT-FOUND
      *
      *   Rev 2.1   26 Jun 2000 10:27:08   T13EX23
      *APA00067 - Neuerstellung
      *
      *   Rev 2.0   Apr 11 2000 16:58:10   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1396                                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: GRAY                                            *
      *    DATUM:    16.06.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00067!16.06.00! 2.1! NEUERSTELLUNG             ! GRAY     *
      * FPA00166!07.07.00! 2.2! RC-NOK JETZT RC-NOT-FOUND ! GRAY     *
      * APA00604!12.04.07! 2.3! ABSCHALTUNG PEW           ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1396.
120407*   05 PI-DRPA1396-PROGRAMM                   PIC  X(008).
          05 PI-DRPA1396-DATEN.
             10 PI-DRPA1396-PGEB-GES                PIC S9(004)
                                                        BINARY.
             10 PI-DRPA1396-PIN                     PIC S9(009)
                                                        BINARY.
      *
       01 PO-DRPA1396.
          05 PO-DRPA1396-RC                         PIC  X(003).
             88 PO-DRPA1396-RC-OK                         VALUE 'OK '.
070700*      88 PO-DRPA1396-RC-NOK                        VALUE 'NOK'.
070700       88 PO-DRPA1396-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1396-DATEN.
120407       10 PGEB-GES               PIC S9(004)       BINARY.
120407       10 ANTA-KUNDE-NAME        PIC  X(026).
120407       10 ANTA-KUNDE-VNAME       PIC  X(026).
120407       10 ANTA-AN-AUFN-DAT       PIC  X(010).
120407       10 ANTA-PIN               PIC S9(009)       BINARY.
120407       10 ANTA-NR                PIC S9(015)       PACKED-DECIMAL.
120407       10 ANTA-AVERM             PIC S9(009)       BINARY.
120407*      10 PO-DRPA1396-PEWA-DATEN.
120407*         15 PO-DRPA1396-PEW-ANTRAG.
120407*            COPY DRPA0434.
120407*         15 PO-DRPA1396-PEWA-LAST-UPDATE     PIC  X(026).
120407*         15 PO-DRPA1396-PEWA-KENNUNG         PIC  X(008).
120407*         15 PO-DRPA1396-PEW-STRUKTUR.
120407*            COPY DRPA0435.
      *
      ****** ENDE COPY-MEMBER ****************************************
