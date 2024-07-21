      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  24.10.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1911.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1911.CPv  $
      *
      *   Rev 2.2   Jan 09 2019 10:03:36   t13ae55
      *APA02711: Zählerüberläufe in PO23 und PMPA0040
      *
      *   Rev 2.1   Nov 12 2018 09:35:16   t13ae55
      *APA02711: Zählerüberläufe in PO23 und PMPA0040
      *
      *   Rev 2.0   Oct 24 2018 14:26:14   t13ae55
      *Archiverstellung - NEUERSTELLUNG
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1922  FUNKTION ALLE PEST ZUR PRINTE      *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MICHAEL NIX                                     *
      *    DATUM:    24.10.2018                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02711!24.10.18! 01 ! NEUERSTELLUNG             ! M. NIX   *
      * APA02711!14.11.17! 02 ! PEBD-TABELLE KORRIGIERT   ! M.NIX    *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA1911.
          05 PI-DRPA1911-PROGRAMM            PIC  X(008).
          05 PI-DRPA1911-UFKT                PIC  9(002).
          05 PI-DRPA1911-DATEN.
             10 PI-DRPA1911-PRINTE           PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA1911.
          05 PO-DRPA1911-RC                  PIC  X(003).
             88 PO-DRPA1911-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1911-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA1911-RC-EOF                        VALUE 'EOF'.
 
          05 PO-DRPA1911-DATEN.
             10 PO-DRPA1911-PEST-PEBD-DATEN.
 
                15 PO-DRPA1911-PEST-DATEN.
      *            DATENSTRUKTUR  PEST TBPA0119
                   COPY DRPA0409.
 
      *            DATENSTRUKTUR  PEBD TBPA0119
                15 PO-DRPA1911-PEBD-DATEN.
                   20 PO-DRPA1911-MAX-USED-PEBD    PIC S9(004) BINARY.
                15 PO-DRPA1911-PEBD-TAB            OCCURS 20.
                    COPY DRPA0414.
      *
      ******* ENDE COPY-MEMBER **************************************

