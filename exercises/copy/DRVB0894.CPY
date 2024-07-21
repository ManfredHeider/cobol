      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.03.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0894.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0894.CPv  $
      *
      *   Rev 2.1   Mar 13 2008 11:05:32   t13ae30
      *AAS00746: Clubprüfung für ernannte Mitglieder
      *
      *   Rev 2.0   04 Mar 2008 14:29:44   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0894                                     *
      *    FUNKTION ERM. ALLE P6 WG.VIRTUELLER PRÜFUNG               *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    04.03.2008                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00746!04.03.08! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0894 DES ZJVB0014                    *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0894.
          05 PI-DRVB0894-PROGRAMM             PIC  X(008).
          05 PI-DRVB0894-UFKT-CODE            PIC  9(002).
      *
       01 PO-DRVB0894.
          05 PO-DRVB0894-RC                   PIC  X(003).
             88 PO-DRVB0894-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0894-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0894-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0894-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0894-DATEN.
             10 PO-DRVB0894-MA-ST-NR          PIC S9(009)  COMP.
      *
      ******** Ende Copy-Member DRVB0894 *****************************
