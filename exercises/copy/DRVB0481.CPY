      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0481.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0481.CPv  $
      *
      *   Rev 2.1   28 Nov 2000 16:32:14   T13AE35
      *ASL00055 - neu; PI/PO für PMSL0012-ZXVB0076
      *Erm. OLZ3
      *
      *   Rev 2.0   19 May 2000 09:49:40   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0481                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0076                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    16.10.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00055!16.10.00! 01 ! NEUERSTELLUNG             ! Walb     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0481                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0481.
          05 PI-DRVB0481-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0481-MONAT              PIC  9(002).
          05 PI-DRVB0481-JAHR               PIC  9(004).
      *
       01 PO-DRVB0481.

          05 PO-DRVB0481-RC                 PIC  X(003).
             88 PO-DRVB0481-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0481-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0481-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0481-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0481-DATEN.
             10 PO-DRVB0481-OBAZ-ST-NR         PIC S9(009)  COMP.
             10 PO-DRVB0481-KNR-WAEHRUNG       PIC S9(004)  COMP.
             10 PO-DRVB0481-KTY-WAEHRUNG       PIC S9(004)  COMP.
             10 PO-DRVB0481-OBAZ-ANSPR-BETRAG  PIC 9(011).
             10 PO-DRVB0481-OBAZ-ANSPR-VOM     PIC X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0481 *****************************
