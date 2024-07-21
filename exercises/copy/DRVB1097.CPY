      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.05.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1097.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1097.CPv  $
      *
      *   Rev 2.1   May 18 2015 10:03:34   t13ae35
      *AVB01347: USt.pfl. Provisionen / VB-System -List-OZP ZXVB0125
      *
      *   Rev 2.0   May 13 2015 13:59:08   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1097                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0125                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.Hering                                         *
      *    DATUM:    13.05.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AVB01347!13.05.13! 2.1! neu, steuerl. Stati je VB ! J.Hering *
      * --------+--------+----+---------------------------+----------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1097                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1097.
          05 PI-DRVB1097-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1097-ST-NR              PIC  S9(009)      BINARY.
      *
       01 PO-DRVB1097.
 
          05 PO-DRVB1097-RC                 PIC  X(003).
             88 PO-DRVB1097-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1097-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1097-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1097-RC-NOT-FOUND                 VALUE 'NFD'.
 
          05 PO-DRVB1097-DATEN.
             10 PO-DRVB1097-ST-NR            PIC S9(009)      BINARY.
             10 PO-DRVB1097-USTAT-DAT-VON    PIC X(026).
             10 PO-DRVB1097-KNR-USTAT-TYP    PIC S9(04)       BINARY.
             10 PO-DRVB1097-KTY-USTAT-TYP    PIC S9(04)       BINARY.
             10 PO-DRVB1097-USTAT-LAST-UPD   PIC X(026).
             10 PO-DRVB1097-USTAT-DAT-BIS    PIC X(010).
             10 PO-DRVB1097-USTAT-AEND-DURCH PIC X(008).
             10 PO-DRVB1097-DAT-VON-EUR      PIC X(010).
      *
      ***** ENDE COPY-MEMBER DRVB1097 *****************************

