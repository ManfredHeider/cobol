      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.03.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0892.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0892.CPv  $
      *
      *   Rev 2.1   Mar 13 2008 11:04:36   t13ae30
      *AAS00746: Clubprüfung für ernannte Mitglieder
      *
      *   Rev 2.0   04 Mar 2008 13:50:10   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0892                                     *
      *    FUNKTION ERM. ERNANNTE MITGLIEDER IM ADMIRALS-CLUB        *
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
      *    FÜR DIE FUNKTION DRVB0892 DES ZJVB0014                    *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0892.
          05 PI-DRVB0892-PROGRAMM             PIC  X(008).
          05 PI-DRVB0892-UFKT-CODE            PIC  9(002).
      *
       01 PO-DRVB0892.
          05 PO-DRVB0892-RC                   PIC  X(003).
             88 PO-DRVB0892-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0892-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0892-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0892-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0892-DATEN.
             10 PO-DRVB0892-MA-ST-NR          PIC S9(009)  COMP.
      *
      ******** Ende Copy-Member DRVB0892 *****************************
