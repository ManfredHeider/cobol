      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1690.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1690.CPv  $
      *
      *   Rev 2.2   Jun 22 2016 14:38:24   T13ex24
      *APA02260: Ergänzung des Produktes 50/91 um die Vorbewegung P
      *S 39
      *
      *   Rev 2.1   Mar 19 2009 16:28:18   t13ex44
      *APA00725: VermögensSparplan - lfd. Verprovisionierung gem. §
      * 125InvG
      *
      *   Rev 2.0   Mar 19 2009 16:02:46   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1690  SELECT AUF TABELLE TBPA0030        *
      *    ZUM VORGEGEBENEN ZEITPUNKT                                *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    19.01.2009                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00725!19.01.09! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA02260!22.06.16! 2.2! ÄNDERUNG WG. TBPA0030     ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1690.
           05 PI-DRPA1690-DATEN.
              10 PI-DRPA1690-PGEB-GES       PIC S9(004)     BINARY.
              10 PI-DRPA1690-PROD-PROD-SCHL PIC S9(004)     BINARY.
220616        10 PI-DRPA1690-ERST-PSCHL-LFDNR
220616                                      PIC S9(004)     BINARY.
              10 PI-DRPA1690-STICHTAG       PIC  X(010).
      *
        01 PO-DRPA1690.
           05 PO-DRPA1690-RC                PIC  X(003).
              88 PO-DRPA1690-RC-OK          VALUE 'OK '.
              88 PO-DRPA1690-RC-NOT-FOUND   VALUE 'NFD'.
           05 PO-DRPA1690-DATEN.
              10 PO-DRPA1690-PGEB-GES       PIC S9(004)     BINARY.
              10 PO-DRPA1690-PROD-PROD-SCHL PIC S9(004)     BINARY.
220616        10 PO-DRPA1690-ERST-PSCHL-LFDNR
220616                                      PIC S9(004)     BINARY.
              10 PO-DRPA1690-PPEZ-GUELTIG-AB
                                            PIC  X(010).
              10 PO-DRPA1690-PPEZ-GUELTIG-BIS
                                            PIC  X(010).
              10 PO-DRPA1690-ERST-PROD-SCHL PIC S9(004)     BINARY.
              10 PO-DRPA1690-PPEZ-LAST-UPDATE
                                            PIC  X(026).
              10 PO-DRPA1690-PPEZ-KENNUNG   PIC  X(008).

