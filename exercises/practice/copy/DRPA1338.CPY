      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1338.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1338.CPv  $
      *
      *   Rev 2.4   Jun 04 2004 09:17:58   t13ex44
      *Korrektur PMPA0057
      *
      *   Rev 2.3   Jun 04 2004 09:07:00   t13ex44
      *Korrektur PMPA0057
      *
      *   Rev 2.2   Jun 03 2004 08:15:14   t13ex44
      *Korrektur PMPA0057
      *
      *   Rev 2.1   25 Apr 2000 10:16:10   T01JMBQ
      *APA00084 - Redesign SST-MA
      *
      *   Rev 2.0   Feb 08 2000 11:30:02   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1338                                      *
      *    --------------------------------------------------------   *
      *    RESTARTINFORMATIONEN ZU PMPA0057 (COPY-4)                  *
      *       FÜR T-PROT3                                             *
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
      * FPA00700!02.06.04!2.2 ! LAENGE 3300               ! SCHEIDEL *
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
       01  DRPA1338-RESTART.
           05 DRPA1338-ID.
              10 FILLER                             PIC  X(008)
                                                      VALUE 'DRPA1338'.
      *
           05 DRPA1338-DATEN.
020604*       10 DRPA1338-PROT-3                    PIC  X(3000).
020604        10 DRPA1338-PROT-3                    PIC  X(3300).
      *
020604     05 FILLER                                PIC  X(692).
      *
      ******* ENDE COPY-MEMBER **************************************
