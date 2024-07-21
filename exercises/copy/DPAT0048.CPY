      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0048.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DPAT0048.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Aug 26 1996 09:58:10   T13EX16
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPAT0048                                      *
      *    --------------------------------------------------------   *
      *    UPRORAHMEN: TRACE BEIM UPRO-START                          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÖLLHAMMER                                     *
      *    DATUM:    12.08.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !12.08.96!    ! NEU                       ! SCHÖLLH. *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIES COPY ENTHÄLT DIE TRACE-AUFRUFE, DIE IM UPRO DIREKT
      *    NACH UPRO-START AUSGEFÜHRT WERDEN.
      *    DIESE AUFRUFE SIND AUSGELAGERT, DAMIT SIE BEI BEDARF
      *    IN PRODUKTION AUS PERFORMANCEGRÜNDEN GEGEN DUMMYS
      *    AUSGETAUSCHT WERDEN KÖNNEN.
      *    --> SIEHE AUCH: DPAT0049 (TRACE UPRO-ENDE)
      *
      ****************************************************************
           MOVE SPACE                          TO ERR-ORT-SEC(01:01)
           MOVE K-PGM-NAME(01:04)              TO ERR-ORT-SEC(02:04)
           PERFORM UR2-TRACE
           MOVE K-PGM-NAME(05:04)              TO ERR-ORT-SEC(01:04)
           MOVE '>'                            TO ERR-ORT-SEC(05:01)
           PERFORM UR2-TRACE
      *
      ******* ENDE COPY-MEMBER **************************************
