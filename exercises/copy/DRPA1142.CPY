      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1142.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1142.CPv  $
      *
      *   Rev 2.1   Mar 30 1999 16:04:16   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:00:04   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1142  FUNKTION                           *
      *                 GET AUSGANGS PE-STUKTUR          (UGPA0002)  *
      *    --------------------------------------------------------- *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    18.12.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!18.12.98! 01  !NEUERSTELLUNG             ! BOHLINGER*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1142                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1142.
          05 PI-DRPA1142-DATEN.
             10 PI-DRPA1142-PRINTE           PIC S9(015)V USAGE COMP-3.
      *
       01 PO-DRPA1142.
          05 PO-DRPA1142-RC                        PIC  X(003).
             88 PO-DRPA1142-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1142-RC-PE-S-NICHT-EIND    VALUE 'PND'.

          05 PO-DRPA1142-MELDUNG.
             10 PO-DRPA1142-MELDUNG-TYP           PIC  X(001).
             10 PO-DRPA1142-MELDUNG-NR            PIC S9(004) BINARY.


          05 PO-DRPA1142-DATEN.
             10 PO-DRPA1142-PEST-ID               PIC S9(15)
                                                  PACKED-DECIMAL.
      *
      ******** ENDE COPY-MEMBER **************************************
