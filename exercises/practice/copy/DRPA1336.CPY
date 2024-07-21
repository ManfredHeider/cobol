      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1336.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1336.CPv  $
      *
      *   Rev 2.5   May 22 2018 10:43:50   T13EX44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.4   Jun 04 2004 09:17:40   t13ex44
      *Korrektur PMPA0057
      *
      *   Rev 2.3   Jun 04 2004 09:06:38   t13ex44
      *Korrektur PMPA0057
      *
      *   Rev 2.2   Jun 03 2004 08:14:50   t13ex44
      *Korrektur PMPA0057
      *
      *   Rev 2.1   25 Apr 2000 10:15:32   T01JMBQ
      *APA00084 - Redesign SST-MA
      *
      *   Rev 2.0   Feb 08 2000 11:30:00   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1336                                      *
      *    --------------------------------------------------------   *
      *    RESTARTINFORMATIONEN ZU PMPA0057 (COPY-2)                  *
      *       FÜR T-PROT1                                             *
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
      * FPA00700!02.06.04!2.2 ! LAENGE 112                ! SCHEIDEL *
      * APA02579!24.04.18!2.5 ! LAENGE 196                ! SCHEIDEL *
      *         !        !    !                           !          *
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
       01  DRPA1336-RESTART.
           05 DRPA1336-ID.
              10 FILLER                             PIC  X(0008)
                                                      VALUE 'DRPA1336'.
      *
           05 DRPA1336-DATEN.
240418        10 DRPA1336-PROT-1                    PIC  X(0196).
      *
240418     05 FILLER                                PIC  X(3796).
      *
      ******* ENDE COPY-MEMBER **************************************

