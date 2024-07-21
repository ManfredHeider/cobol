      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.05.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1099.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1099.CPv  $
      *
      *   Rev 2.1   Jun 02 2015 15:01:08   t13ex71
      *AVB01347: Unterprogramm UPVB0189 zum Ermitteln steuerlicher 
      *Status für Daisy
      *
      *   Rev 2.0   May 20 2015 10:36:08   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1099                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1099                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    20.05.2015                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01347!20.05.15! 01 ! NEU                       !B.PRETSCH *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1099: ERMITTLUNG DES STEUERLICHEN
      *                               STATUS FÜR DAISY
      *
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRVB1099.
           05  PI-DRVB1099-DATEN.
               10 PI-DRVB1099-VBNR             PIC  9(007).
      *
       01  PO-DRVB1099.
           05  PO-DRVB1099-RC                  PIC  X(003).
      *--- Ermittlung steuerl. Status O.K.
               88 PO-DRVB1099-RC-OK                        VALUE 'OK '.
      *--- VBNR in ST-Tabelle nicht gefunden
               88 PO-DRVB1099-RC-NFD                       VALUE 'NFD'.
      *
           05  PO-DRVB1099-DATEN.
               10 PO-DRVB1099-STRL-STA         PIC  9(004).
               10 PO-DRVB1099-STRL-STA-TXT     PIC  X(025).
               10 PO-DRVB1099-ST-TYP           PIC  9(004).
               10 PO-DRVB1099-ST-TYP-BEZ       PIC  X(025).
      *
      ******************** ENDE COPY-MEMBER ***************************

