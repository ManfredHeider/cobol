      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1413.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA1413.CPv  $
      *
      *   Rev 2.2   Jan 30 2002 17:42:02   T13ex22
      *APA00173: Abweichung bei Vorbewegungsermittlung zugelassen
      *
      *   Rev 2.1   30 Aug 2000 11:15:06   T13EX22
      *APA00105: PVW III
      *
      *   Rev 2.0   12 Jul 2000 12:32:40   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1413                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1413                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    07.08.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00105!07.08.00!2.0 ! NEU                       ! MUND     *
      * APA00173!11.01.02!2.1 ! PI-DRPA1413-ABWEICHUNG    ! MUND     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1413
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1413.
           05 PI-DRPA1413-PRINTE          PIC S9(015) PACKED-DECIMAL.
           05 PI-DRPA1413-PGEB-GES        PIC S9(004) BINARY.
           05 PI-DRPA1413-PROD-PROD-SCHL  PIC S9(004) BINARY.
           05 PI-DRPA1413-FAELLIG-VON     PIC  X(010).
           05 PI-DRPA1413-GSMT-PROV   PIC S9(009)V9(2) PACKED-DECIMAL.
110102     05 PI-DRPA1413-ABWEICHUNG  PIC S9(002)V9(2) PACKED-DECIMAL.

       01  PO-DRPA1413.
           05 PO-DRPA1413-RC              PIC  X(003).
              88 PO-DRPA1413-RC-OK                        VALUE 'OK '.
              88 PO-DRPA1413-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05 PO-DRPA1413-SUM-GSMT-PROV PIC S9(009)V9(2) PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************

