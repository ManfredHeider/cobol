      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0376.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0376.cpv  $
      *
      *   Rev 2.1   Jun 07 1999 14:51:32   T13AE42
      *AVB00445 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   Mar 17 1999 10:59:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0376                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0376                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    04.06.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00445!04.06.99! 01 ! NEU                       ! SCHMIDT  *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0376: ..
      *        -   -------------------
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
      *
       01 PI-DRVB0376.
          05 PI-DRVB0376-MA-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB0376.

          05 PO-DRVB0376-RC                 PIC  X(003).
             88 PO-DRVB0376-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0376-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0376-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0376-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0376-DATEN.
             10 PO-DRVB0376-MS-NR            PIC S9(004) COMP.
             10 PO-DRVB0376-BS-NR            PIC S9(004) COMP.
             10 PO-DRVB0376-MSBZ-DAT-BIS     PIC  X(010).
             10 PO-DRVB0376-MSBZ-DAT-VON     PIC  X(026).
      *
      ***** ENDE COPY-MEMBER DRVB0376 *****************************
