      * T01JM2E   Stand: 05/01/99    14:59:52
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1070.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1070.CPv  $
      *
      *   Rev 2.2   25 Apr 2000 10:14:58   T01JMBQ
      *APA00084 - Redesign SST-MA
      *
      *   Rev 2.1   May 27 1999 10:44:50   T01JM2E
      *NT-Umstellung
      *
      *   Rev 1.0   20 Jul 1998 10:19:52   P13SP99
      *Initial revision.
      *
      *   ARBEITSSATZ SCHNITTSTELLE MA-ABRECHNUNG
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1070                                      *
      *    --------------------------------------------------------   *
      *    BEWEGUNGSDATEN MIT EINER PE-STRUKTUR AUS PAMS              *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILHELM                                          *
      *    DATUM:    27.03.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY   !27.03.99!2.1 ! NEU                       ! WILHELM  *
      *         !        !    !                           !          *
      * APA00084!14.03.00!2.1 ! KOMPLETTE ÜBERARBEITUNG   ! KRUFF    *
      *         !        !    ! - PAMS-BEWEGUNGSDATEN IN  !          *
      *         !        !    !   COPY-STRECKE DRPA1347   !          *
      *         !        !    ! - PAMS-STRUKTURDATEN IN   !          *
      *         !        !    !   COPY-STRECKE DRPA1005   !          *
      *         !        !    ! - PE-STRUKTUR-DATEN-LEN   !          *
      *         !        !    !   ENTFÄLLT                !          *
      *         !        !    ! - KOMPLETTE ÄNDERUNG DER  !          *
      *         !        !    !   STUFENNUMMERN           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN DER PAMS-BEWEGUNGSDATEN     *
      *    SOWIE EINER PE-STRUKTUR                                   *
      *                                                              *
      ****************************************************************
      *
           25 PAMS-DATEN-EINE-PE.
              27 FILLER.
                 COPY DRPA1347.
              27 FILLER.
                 COPY DRPA1005.
      *
      ******* ENDE COPY-MEMBER **************************************
