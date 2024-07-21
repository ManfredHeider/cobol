      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.06.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1845.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1845.CPv  $
      *
      *   Rev 2.1   Jun 22 2016 15:20:36   T13ex24
      *APA02260: Ergänzung des Produktes 50/91 um die Vorbewegung P
      *S 39
      *
      *   Rev 2.0   Jun 22 2016 14:16:56   T13ex24
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1845  LESEN MIT CURSOR TABELLE TBPA0030  *
      *    ZUM VORGEGEBENEN ZEITPUNKT                                *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KÖPRÜCÜ                                         *
      *    DATUM:    22.06.2016                                      *
      ****************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02260!22.06.16! 2.0! NEUERSTELLUNG             ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
        01 PI-DRPA1845.
           05 PI-DRPA1845-PROGRAMM          PIC  X(008).
           05 PI-DRPA1845-UFKT-CODE         PIC  9(002).
           05 PI-DRPA1845-DATEN.
              10 PI-DRPA1845-PGEB-GES       PIC S9(004)     BINARY.
              10 PI-DRPA1845-PROD-PROD-SCHL PIC S9(004)     BINARY.
              10 PI-DRPA1845-ERST-PSCHL-LFDNR
                                            PIC S9(004)     BINARY.
              10 PI-DRPA1845-STICHTAG       PIC  X(010).
      *
        01 PO-DRPA1845.
           05 PO-DRPA1845-RC                PIC  X(003).
              88 PO-DRPA1845-RC-OK          VALUE 'OK '.
              88 PO-DRPA1845-RC-LEER        VALUE 'LER'.
              88 PO-DRPA1845-RC-EOF         VALUE 'EOF'.
           05 PO-DRPA1845-DATEN.
              10 PO-DRPA1845-PGEB-GES       PIC S9(004)     BINARY.
              10 PO-DRPA1845-PROD-PROD-SCHL PIC S9(004)     BINARY.
              10 PO-DRPA1845-ERST-PSCHL-LFDNR
                                            PIC S9(004)     BINARY.
              10 PO-DRPA1845-PPEZ-GUELTIG-AB
                                            PIC  X(010).
              10 PO-DRPA1845-PPEZ-GUELTIG-BIS
                                            PIC  X(010).
              10 PO-DRPA1845-ERST-PROD-SCHL PIC S9(004)     BINARY.
              10 PO-DRPA1845-PPEZ-LAST-UPDATE
                                            PIC  X(026).
              10 PO-DRPA1845-PPEZ-KENNUNG   PIC  X(008).

