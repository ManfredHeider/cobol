      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0623.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0623.CPv  $
      *
      *   Rev 2.1   May 27 1999 13:42:34   T01JM2S
      *APA00042
      *
      *   Rev 2.0   Feb 27 1999 14:29:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:55:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0623 ERMITTELN ALLE DAISY-MELDUNGEN ZU    *
      *                          EINER PROVISIONSMELDUNG              *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ALLE DAISY MELD     >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                          *
      *    DATUM:    12.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!12.11.97! 01 ! NEU                       ! PRIGGEN  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0623                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0623.
          05 PI-DRPA0623-DATEN.
             10 PI-DRPA0623-PMBE-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0623-PMBE-VERSION            PIC S9(004) BINARY.
      *
       01 PO-DRPA0623.
          05 PO-DRPA0623-RC                         PIC  X(003).
             88 PO-DRPA0623-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0623-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0623-DATEN.
             10 PO-DRPA0623-MELD.
                COPY DRPA0615.
      *
      ******* ENDE COPY-MEMBER **************************************
