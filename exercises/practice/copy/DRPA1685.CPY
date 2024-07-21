      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1685.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1685.CPv  $
      *
      *   Rev 2.3   Jun 22 2016 13:14:46   T13ex24
      *APA02260: Ergänzung des Produktes 50/91 um die Vorbewegung P
      *S 39
      *
      *   Rev 2.2   Jun 22 2016 12:37:32   T13ex24
      *APA02260: Ergänzung des Produktes 50/91 um die Vorbewegung P
      *S 39
      *
      *   Rev 2.1   Mar 04 2009 15:27:40   t13ex44
      *APA00725: VermögensSparplan - lfd. Verprovisionierung gem. §
      * 125InvG
      *
      *   Rev 2.0   Jan 19 2009 09:16:52   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1685  DELETE AUF TABELLE TBPA0030        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    19.01.2009                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00725!19.01.09! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA02260!22.06.16! 2.2! ERWEITERUNG PK UM LFDNR   ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1685.
           05 PI-DRPA1685-DATEN.
              10 PI-DRPA1685-PGEB-GES       PIC S9(004)     BINARY.
              10 PI-DRPA1685-PROD-PROD-SCHL PIC S9(004)     BINARY.
220616        10 PI-DRPA1685-ERST-PSCHL-LFDNR
220616                                      PIC S9(004)     BINARY.
              10 PI-DRPA1685-PPEZ-GUELTIG-AB
                                            PIC  X(010).
      *
        01 PO-DRPA1685.
           05 PO-DRPA1685-RC                PIC  X(003).
              88 PO-DRPA1685-RC-OK          VALUE 'OK '.
              88 PO-DRPA1685-RC-NOT-FOUND   VALUE 'NFD'.

