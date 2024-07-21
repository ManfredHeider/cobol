      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1340.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1340.CPv  $
      *
      *   Rev 2.4   Jun 04 2004 09:18:18   t13ex44
      *Korrektur PMPA0057
      *
      *   Rev 2.3   Jun 04 2004 09:07:20   t13ex44
      *Korrektur PMPA0057
      *
      *   Rev 2.2   Jun 03 2004 08:15:34   t13ex44
      *Korrektur PMPA0057
      *
      *   Rev 2.1   25 Apr 2000 10:16:44   T01JMBQ
      *APA00084 - Redesign SST-MA
      *
      *   Rev 2.0   Mar 01 2000 09:57:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1340                                      *
      *    --------------------------------------------------------   *
      *    RESTARTINFORMATIONEN ZU PMPA0057 (COPY-6)                  *
      *       FÜR T-PROT5                                             *
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
       01  DRPA1340-RESTART.
           05 DRPA1340-ID.
              10 FILLER                             PIC  X(008)
                                                      VALUE 'DRPA1340'.
      *
           05 DRPA1340-DATEN.
020604*       10 DRPA1340-PROT-5                    PIC  X(3000).
020604        10 DRPA1340-PROT-5                    PIC  X(3300).
      *
020604     05 FILLER                                PIC  X(692).
      *
      ******* ENDE COPY-MEMBER **************************************
