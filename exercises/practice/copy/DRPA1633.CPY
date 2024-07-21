      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.10.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1633.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1633.CPv  $
      *
      *   Rev 2.3   Mar 19 2009 08:25:26   t13ex44
      *APA00868: Erweiterung Prov- und Rück-DTA Generali
      *
      *   Rev 2.2   Oct 21 2004 13:55:18   t13ex44
      *APA00295: Daisy-KÜ
      *
      *   Rev 2.1   Oct 18 2004 13:33:14   t13ex44
      *APA00295: Daisy-KÜ
      *
      *   Rev 2.0   Oct 15 2004 15:12:24   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1633                                     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    12.10.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00295!12.10.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA00868!03.02.09! 2.3! NEUE FUNTKION DRPA1688    ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE FUNKTIONEN FÜR DAS JOIN-OZP       *
      *    ZJPA0019                                                  *
      *                                                              *
      ****************************************************************
      *
        01 PI-ZJPA0019.
           05 PI-ZJPA0019-FUNKTION     PIC X(008).
              88 PI-ZJPA0019-DRPA1634      VALUE 'DRPA1634'.
              88 PI-ZJPA0019-DRPA1635      VALUE 'DRPA1635'.
030209        88 PI-ZJPA0019-DRPA1688      VALUE 'DRPA1688'.
