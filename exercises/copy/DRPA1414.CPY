      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1414.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA1414.CPv  $
      *
      *   Rev 2.2   Jan 30 2002 17:42:02   T13ex22
      *APA00173: Abweichung bei Vorbewegungsermittlung zugelassen
      *
      *   Rev 2.1   09 Aug 2000 08:21:42   T13EX22
      *APA00098: Neue Funktion: PIN-Prüfung für iii
      *
      *   Rev 2.0   12 Jul 2000 12:32:42   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1414                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1414                    *
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
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1414
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1414.
           05 PI-DRPA1414-PRINTE          PIC S9(015) PACKED-DECIMAL.
           05 PI-DRPA1414-PGEB-GES        PIC S9(004) BINARY.
           05 PI-DRPA1414-PROD-PROD-SCHL  PIC S9(004) BINARY.
           05 PI-DRPA1414-FAELLIG-VON     PIC  X(010).
           05 PI-DRPA1414-GSMT-PROV   PIC S9(009)V9(2) PACKED-DECIMAL.
110102     05 PI-DRPA1414-ABWEICHUNG  PIC S9(002)V9(2) PACKED-DECIMAL.

       01  PO-DRPA1414.
           05 PO-DRPA1414-RC              PIC  X(003).
              88 PO-DRPA1414-RC-OK                        VALUE 'OK '.
              88 PO-DRPA1414-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05 PO-DRPA1414-SUM-GSMT-PROV PIC S9(009)V9(2) PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER **************************************


