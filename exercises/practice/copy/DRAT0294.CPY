      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0294.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drat0294.cpv  $
      *
      *   Rev 2.1   Jun 09 1999 13:29:48   T01JM2S
      *APA00012 - UMSETZUNG FACHL. ANFORDERUNG FKPAG026
      *
      *   Rev 2.0   Mar 16 1999 09:57:18   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0294                                      *
      *    --------------------------------------------------------   *
      *    RESTARTINFORMATIONEN ZU PMPA0046                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    07.06.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!07.06.99!2.1 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN MIT DEN RESTARTINFORMA-     *
      *    TIONEN ZU PMPA0046.                                       *
      *                                                              *
      ****************************************************************
      *
       01  DRAT0294-RESTART.
           05 DRAT0294-ID.
              10 FILLER                             PIC  X(008)
                                                      VALUE 'DRAT0294'.
      *
           05 DRAT0294-DATEN.
              10 DRAT0294-PMPG-ID                   PIC S9(015)
                                                        PACKED-DECIMAL.
              10 DRAT0294-ANZ-EFILE1                PIC S9(009)
                                                        PACKED-DECIMAL.
