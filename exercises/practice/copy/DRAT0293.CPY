      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0293.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drat0293.cpv  $
      *
      *   Rev 2.1   Jun 02 1999 08:16:02   T13EX22
      *Übergabe an JJÄ
      *
      *   Rev 2.0   Mar 16 1999 09:57:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0293                                      *
      *    --------------------------------------------------------   *
      *    RESTARTINFORMATIONEN ZU PMPA0042                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    30.04.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.02.98! 1.0! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN MIT DEN RESTARTINFORMA-     *
      *    TIONEN ZU PMPA0041 (UMNUMMERIERUNG DAISY VERTRAGSDATEN)   *
      *                                                              *
      ****************************************************************
      *
       01  DRAT0293-RESTART.
           05 DRAT0293-ID.
              10 FILLER                             PIC  X(008)
                                                      VALUE 'DRAT0293'.
      *
           05 DRAT0293-DATEN.
240998        10 DRAT0293-LAST-VNR-READ             PIC X(015).
080299        10 DRAT0293-ANZ-VNR-READ              PIC X(015).
              10 DRAT0293-ANZ-VNR-CHANGED           PIC X(015).
              10 DRAT0293-ANZ-VNR-NOT-FOUND         PIC X(015).
