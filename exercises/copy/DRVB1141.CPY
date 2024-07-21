      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1141.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1141.CPv  $
      *
      *   Rev 2.1   Jun 30 2016 11:51:08   t13ex71
      *AVB01400: Projekt Lucy2 - Neue Funktion in UPVB0189 aufnehme
      *n (BPR)
      *
      *   Rev 2.0   May 24 2016 16:23:02   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1141                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1141                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    24.05.2016                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01400!24.05.16! 01 ! NEU                       !B.PRETSCH *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1141: ERMITTLUNG DES STEUERLICHEN
      *                               STATUS FÜR FIBU
      *
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRVB1141.
           05  PI-DRVB1141-DATEN.
               10 PI-DRVB1141-VBNR             PIC  9(007).
      *
       01  PO-DRVB1141.
           05  PO-DRVB1141-RC                  PIC  X(003).
      *--- Ermittlung steuerl. Status O.K.
               88 PO-DRVB1141-RC-OK                        VALUE 'OK '.
      *--- VBNR in ST-Tabelle nicht gefunden
               88 PO-DRVB1141-RC-NFD                       VALUE 'NFD'.
      *
           05  PO-DRVB1141-DATEN.
               10 PO-DRVB1141-STRL-STA         PIC  9(004).
               10 PO-DRVB1141-STRL-STA-TXT     PIC  X(025).
               10 PO-DRVB1141-ST-TYP           PIC  9(004).
               10 PO-DRVB1141-ST-TYP-BEZ       PIC  X(025).
      *
      ******************** ENDE COPY-MEMBER ***************************

