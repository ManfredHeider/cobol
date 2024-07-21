      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0618.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0618.CPv  $
      *
      *   Rev 2.1   Mar 15 1999 16:15:18   T13EX22
      *MFE-UMSTELLUNG
      *
      *   Rev 2.0   Feb 27 1999 14:29:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:55:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0618 ERMITTELN DER AKTUELLSTEN MELDUNG    *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<AKT MELD ERMITTELN >>                    *
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
      *    FÜR DIE FUNKTION DRPA0618                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0618.
          05 PI-DRPA0618-DATEN.
             10 PI-DRPA0618-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA0618.
          05 PO-DRPA0618-RC                         PIC  X(003).
             88 PO-DRPA0618-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0618-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0618-DATEN.
             10 PO-DRPA0618-MELD.
                11 PO-DRPA0618-PMBE.
                   COPY DRPA0411.
                11 PO-DRPA0618-PMPR.
                   COPY DRPA0432.
                11 PO-DRPA0618-PMPT.
                   COPY DRPA0421.
      *
      ******* ENDE COPY-MEMBER **************************************
