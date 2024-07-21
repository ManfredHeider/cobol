      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.11.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1157.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1157.CPv  $
      *
      *   Rev 2.1   Dec 08 2016 15:14:10   t13ex71
      *AVB01400: Projekt Lucy2 - Neue Funktion 3 in UPVB0189 für Fi
      *bu Jahresultimo aufnehmen (BPR)
      *
      *   Rev 2.0   Nov 04 2016 13:24:20   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1157                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1157                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    30.11.2016                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01400!30.11.16! 01 ! NEU                       !B.PRETSCH *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1157: ERMITTLUNG DES STEUERLICHEN
      *                               STATUS FÜR FIBU ZUR
      *                               JAHRESULTIMO-ABRECHNUNG
      *
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRVB1157.
           05  PI-DRVB1157-DATEN.
               10 PI-DRVB1157-VBNR             PIC  9(007).
               10 PI-DRVB1157-JAHR             PIC  9(004).
      *
       01  PO-DRVB1157.
           05  PO-DRVB1157-RC                  PIC  X(003).
      *--- Ermittlung steuerl. Status O.K.
               88 PO-DRVB1157-RC-OK                        VALUE 'OK '.
      *--- VBNR in ST-Tabelle nicht gefunden
               88 PO-DRVB1157-RC-NFD                       VALUE 'NFD'.
      *
           05  PO-DRVB1157-DATEN.
               10 PO-DRVB1157-STRL-STA         PIC  9(004).
               10 PO-DRVB1157-STRL-STA-TXT     PIC  X(025).
               10 PO-DRVB1157-ST-TYP           PIC  9(004).
               10 PO-DRVB1157-ST-TYP-BEZ       PIC  X(025).
      *
      ******************** ENDE COPY-MEMBER ***************************
 

