      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.05.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0777.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0777.CPv  $
      *
      *   Rev 2.1   Jul 07 2005 11:57:06   t13ae30
      *AVD00009: Projekt VBI: SST Clubprüfung
      *
      *   Rev 2.0   May 06 2005 12:21:56   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0777                                     *
      *    FUNKTION ERMITTELN OB FÜR EINEN VB EIN BEENDETER          *
      *    MAST-EINTRAG ZU EINEM GELIEFERTEN KNR_TYP VORHANDEN IST   *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    06.05.2005                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVD00009!04.04.05! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0777                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0777.
          05 PI-DRVB0777-PROGRAMM           PIC  X(008).
          05 PI-DRVB0777-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0777-MA-ST-NR           PIC S9(009)    COMP.
          05 PI-DRVB0777-KTY-MAST-TYP       PIC S9(004)    COMP.
          05 PI-DRVB0777-KNR-MAST-TYP       PIC S9(004)    COMP.
      *
       01 PO-DRVB0777.
          05 PO-DRVB0777-RC                 PIC  X(005).
             88 PO-DRVB0777-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0777-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB0777-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0777-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0777-RC-EOF                       VALUE 'EOF'.

      *
      ******** Ende Copy-Member DRVB0777 *****************************
