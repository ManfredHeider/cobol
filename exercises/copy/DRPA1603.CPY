      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1603.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1603.CPv  $
      *
      *   Rev 2.2   Dec 17 2004 10:02:06   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.1   Oct 12 2004 08:15:02   t13ex44
      *APA00218: Auslagerung von Provisionsdaten
      *
      *   Rev 2.0   Jul 16 2004 15:41:16   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1603  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    15.07.2004                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00218!15.07.04! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE RESTART-INFORMATIONEN             *
      *    FÜR DAS PROGRAMM PMPA0078 (AUSLAGERUNG DAISY-MELDUNGEN)   *
      *                                                              *
      ****************************************************************
      *
        01 DRPA1603-RESTART.
           05 DRPA1603-ID              PIC  X(008)
                                       VALUE 'DRPA1603'.
      *
           05 DRPA1603-PGEB-GES        PIC S9(004)       BINARY.
           05 DRPA1603-AUSLAGERN-BIS   PIC  X(010).
           05 DRPA1603-PRINTE-VON      PIC S9(015)       PACKED-DECIMAL.
           05 DRPA1603-PRINTE-BIS      PIC S9(015)       PACKED-DECIMAL.
           05 DRPA1603-PRINTE-LAST     PIC S9(015)       PACKED-DECIMAL.
           05 DRPA1603-ERSTELLT        PIC  X(026).
           05 DRPA1603-VORL-STATUS     PIC  X(001).
           05 DRPA1603-ZAEHLER.
              10 DRPA1603-ANZ-EIN1     PIC S9(016)       BINARY.
              10 DRPA1603-ANZ-EIN2     PIC S9(016)       BINARY.
              10 DRPA1603-ANZ-EIN3     PIC S9(016)       BINARY.
              10 DRPA1603-ANZ-PMBE     PIC S9(016)       BINARY.
              10 DRPA1603-ANZ-PMBED    PIC S9(016)       BINARY.
              10 DRPA1603-ANZ-PMBEH    PIC S9(016)       BINARY.
              10 DRPA1603-ANZ-PMPR     PIC S9(016)       BINARY.
              10 DRPA1603-ANZ-PMPRD    PIC S9(016)       BINARY.
              10 DRPA1603-ANZ-PMPRH    PIC S9(016)       BINARY.
              10 DRPA1603-ANZ-PMPT     PIC S9(016)       BINARY.
              10 DRPA1603-ANZ-PMPTD    PIC S9(016)       BINARY.
              10 DRPA1603-ANZ-PMPTH    PIC S9(016)       BINARY.
              10 DRPA1603-ANZ-PDPT     PIC S9(016)       BINARY.
              10 DRPA1603-ANZ-PDPTD    PIC S9(016)       BINARY.
              10 DRPA1603-ANZ-PDPTH    PIC S9(016)       BINARY.
