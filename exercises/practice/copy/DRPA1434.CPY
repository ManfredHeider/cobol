      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1434.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1434.CPv  $
      *
      *   Rev 2.2   16 May 2007 06:46:06   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.1   22 Nov 2000 10:33:40   T13EX21
      *APA00116 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:33:04   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1434                                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    21.11.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00116!21.11.00! 2.1! NEUERSTELLUNG             ! STORCH   *
      * APA00604!12.04.07! 2.2! ABSCHALTUNG PEW           ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1434.
          05 PI-DRPA1434-DATEN.
             10 PI-DRPA1434-PGEB-GES      PIC S9(004) BINARY.
             10 PI-DRPA1434-VNR           PIC  X(015).
      *
       01 PO-DRPA1434.
          05 PO-DRPA1434-RC                         PIC  X(003).
             88 PO-DRPA1434-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1434-RC-NOT-FOUND                  VALUE 'NFD'.
      *
          05 PO-DRPA1434-DATEN.
120407       10 PGEB-GES               PIC S9(004)       BINARY.
120407       10 ANTA-KUNDE-NAME        PIC  X(026).
120407       10 ANTA-KUNDE-VNAME       PIC  X(026).
120407       10 ANTA-AN-AUFN-DAT       PIC  X(010).
120407       10 ANTA-PIN               PIC S9(009)       BINARY.
120407       10 ANTA-NR                PIC S9(015)       PACKED-DECIMAL.
120407       10 ANTA-AVERM             PIC S9(009)       BINARY.
120407*      10 PO-DRPA1434-PEWA-DATEN.
120407*         15 PO-DRPA1434-PEW-ANTRAG.
120407*            COPY DRPA0434.
120407*         15 PO-DRPA1434-PEWA-LAST-UPDATE     PIC  X(026).
120407*         15 PO-DRPA1434-PEWA-KENNUNG         PIC  X(008).
120407*         15 PO-DRPA1434-PEW-STRUKTUR.
120407*            COPY DRPA0435.
      *
      ****** ENDE COPY-MEMBER ****************************************
