      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1263.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1263.cpv  $
      *
      *   Rev 2.1   Jun 19 1999 11:14:24   T13EX22
      *CRPA0069 umgesetzt
      *
      *   Rev 2.0   Mar 30 1999 08:44:18   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:48   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1263 ERMITTELN GRÖßTE REIHENFOLGENR. ZUR  *
      *                          FÄLLIGKEIT                           *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF DRPA1263                                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    12.06.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!12.06.99! 01 ! NEU                       ! MUND     *
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
       01 PI-DRPA1263.
          05 PI-DRPA1263-DATEN.
             10 PI-DRPA1263-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1263-FAELLIG-VON             PIC  X(010).
      *
       01 PO-DRPA1263.
          05 PO-DRPA1263-RC                         PIC  X(003).
             88 PO-DRPA1263-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1263-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1263-DATEN.
             10 PO-DRPA1263-PMBE-RFOLGE-NR           PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************

