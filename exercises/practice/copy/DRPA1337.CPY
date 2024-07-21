      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1337.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1337.CPv  $
      *
      *   Rev 2.4   Jun 04 2004 09:17:50   t13ex44
      *Korrektur PMPA0057
      *
      *   Rev 2.3   Jun 04 2004 09:06:50   t13ex44
      *Korrektur PMPA0057
      *
      *   Rev 2.2   Jun 03 2004 08:15:04   t13ex44
      *Korrektur PMPA0057
      *
      *   Rev 2.1   25 Apr 2000 10:15:52   T01JMBQ
      *APA00084 - Redesign SST-MA
      *
      *   Rev 2.0   Feb 08 2000 11:30:00   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1337                                      *
      *    --------------------------------------------------------   *
      *    RESTARTINFORMATIONEN ZU PMPA0057 (COPY-3)                  *
      *       FÜR T-PROT2                                             *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    01.03.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00084!01.03.00! 01 ! NEU                       ! KRUFF    *
      * FPA00700!02.06.04!2.2 ! LAENGE 3100               ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN MIT DEN RESTARTINFORMA-     *
      *    TIONEN ZU PMPA0057 (SST-MA).                              *
      *                                                              *
      ****************************************************************
      *
       01  DRPA1337-RESTART.
           05 DRPA1337-ID.
              10 FILLER                             PIC  X(008)
                                                      VALUE 'DRPA1337'.
      *
           05 DRPA1337-DATEN.
020604*       10 DRPA1337-PROT-2                    PIC  X(3000).
020604        10 DRPA1337-PROT-2                    PIC  X(3100).
      *
020604     05 FILLER                                PIC  X(892).
      *
      ******* ENDE COPY-MEMBER **************************************
