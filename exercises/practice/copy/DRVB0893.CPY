      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.03.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0893.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0893.CPv  $
      *
      *   Rev 2.1   Mar 13 2008 11:05:06   t13ae30
      *AAS00746: Clubprüfung für ernannte Mitglieder
      *
      *   Rev 2.0   04 Mar 2008 13:50:18   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0893                                     *
      *    FUNKTION ERM. ERNANNTE MITGL.IN 'BES.LEIST.IM AS-CLUB'    *
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
      *    FÜR DIE FUNKTION DRVB0893 DES ZJVB0014                    *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0893.
          05 PI-DRVB0893-PROGRAMM             PIC  X(008).
          05 PI-DRVB0893-UFKT-CODE            PIC  9(002).
      *
       01 PO-DRVB0893.
          05 PO-DRVB0893-RC                   PIC  X(003).
             88 PO-DRVB0893-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0893-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0893-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0893-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0893-DATEN.
             10 PO-DRVB0893-MA-ST-NR          PIC S9(009)  COMP.
      *
      ******** Ende Copy-Member DRVB0893 *****************************
