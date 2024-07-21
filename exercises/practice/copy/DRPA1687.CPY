      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1687.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1687.CPv  $
      *
      *   Rev 2.2   Jun 22 2016 14:51:50   T13ex24
      *APA02260: Ergänzung des Produktes 50/91 um die Vorbewegung P
      *S 39
      *
      *   Rev 2.1   Mar 04 2009 15:27:50   t13ex44
      *APA00725: VermögensSparplan - lfd. Verprovisionierung gem. §
      * 125InvG
      *
      *   Rev 2.0   Jan 19 2009 14:49:06   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1687  LESEN ERSTPRODUKT AUS KONFIGURATION*
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
        01 PI-DRPA1687.
220616     05 PI-DRPA1687-PROGRAMM          PIC  X(008).
220616     05 PI-DRPA1687-UFKT-CODE         PIC  9(002).
           05 PI-DRPA1687-DATEN.
              10 PI-DRPA1687-PGEB-GES       PIC S9(004)     BINARY.
              10 PI-DRPA1687-PROD-PROD-SCHL PIC S9(004)     BINARY.
220616        10 PI-DRPA1687-ERST-PSCHL-LFDNR
220616                                      PIC S9(004)     BINARY.
              10 PI-DRPA1687-STICHTAG       PIC  X(010).
      *
        01 PO-DRPA1687.
           05 PO-DRPA1687-RC                PIC  X(003).
              88 PO-DRPA1687-RC-OK          VALUE 'OK '.
              88 PO-DRPA1687-RC-LEER        VALUE 'LER'.
              88 PO-DRPA1687-RC-EOF         VALUE 'EOF'.
           05 PO-DRPA1687-DATEN.
              10 PO-DRPA1687-ERST-PROD-SCHL PIC S9(004)     BINARY.

