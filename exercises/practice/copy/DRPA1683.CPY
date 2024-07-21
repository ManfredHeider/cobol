      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1683.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1683.CPv  $
      *
      *   Rev 2.1   Mar 04 2009 15:27:30   t13ex44
      *APA00725: VermögensSparplan - lfd. Verprovisionierung gem. §
      * 125InvG
      *
      *   Rev 2.0   Jan 19 2009 09:13:28   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1683  FUNKTIONSAUFRUFE FÜR ZTPA0030      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    19.01.2009                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00725!19.01.09! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
        01 PI-ZTPA0030.
           05 PI-ZTPA0030-FUNKTION     PIC  X(008).
              88 PI-ZTPA0030-DRPA1684-INS   VALUE 'DRPA1684'.
              88 PI-ZTPA0030-DRPA1685-DEL   VALUE 'DRPA1685'.
              88 PI-ZTPA0030-DRPA1686-SEL-ALL
                                            VALUE 'DRPA1686'.
